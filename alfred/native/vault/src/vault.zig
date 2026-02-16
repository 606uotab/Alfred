const std = @import("std");
const crypto = @import("crypto.zig");

pub const Secret = struct {
    key: []const u8,
    value: []const u8,
};

pub const Note = struct {
    id: u64,
    text: []const u8,
    created_at: i64,
};

const VaultData = struct {
    secrets: []const Secret,
    notes: []const Note,
};

pub const Vault = struct {
    allocator: std.mem.Allocator,
    vault_path: []const u8,
    locked: bool,
    key: [crypto.key_len]u8,
    salt: [crypto.salt_len]u8,
    data_json: ?[]u8,

    pub fn init(allocator: std.mem.Allocator, vault_path: []const u8) Vault {
        return .{
            .allocator = allocator,
            .vault_path = vault_path,
            .locked = true,
            .key = undefined,
            .salt = undefined,
            .data_json = null,
        };
    }

    fn fileExists(self: *const Vault) bool {
        const file = std.fs.openFileAbsolute(self.vault_path, .{}) catch return false;
        file.close();
        return true;
    }

    /// Create a new vault with the given master password.
    pub fn initVault(self: *Vault, password: []const u8) !void {
        if (self.fileExists()) return error.VaultAlreadyExists;

        const salt = crypto.generateSalt();
        const key = crypto.deriveKey(password, &salt);
        const empty = "{\"secrets\":[],\"notes\":[]}";

        const encrypted = try crypto.encrypt(self.allocator, empty, key);
        defer self.allocator.free(encrypted);

        try self.saveFile(&salt, encrypted);

        self.salt = salt;
        self.key = key;
        self.data_json = try self.allocator.dupe(u8, empty);
        self.locked = false;
    }

    /// Unlock the vault with the master password.
    pub fn unlock(self: *Vault, password: []const u8) !void {
        if (!self.locked) return error.AlreadyUnlocked;
        if (!self.fileExists()) return error.VaultNotFound;

        const file_data = try self.readFile();
        defer self.allocator.free(file_data.encrypted);

        const key = crypto.deriveKey(password, &file_data.salt);

        const decrypted = crypto.decrypt(self.allocator, file_data.encrypted, key) catch
            return error.WrongPassword;

        self.salt = file_data.salt;
        self.key = key;
        self.data_json = decrypted;
        self.locked = false;
    }

    /// Lock the vault — zero out sensitive data.
    pub fn lock(self: *Vault) void {
        if (self.data_json) |d| {
            @memset(d, 0);
            self.allocator.free(d);
        }
        self.data_json = null;
        @memset(&self.key, 0);
        self.locked = true;
    }

    /// Store or update a secret.
    pub fn store(self: *Vault, key_name: []const u8, value: []const u8) !void {
        if (self.locked) return error.VaultLocked;

        const parsed = try std.json.parseFromSlice(VaultData, self.allocator, self.data_json.?, .{});
        defer parsed.deinit();

        var secrets = std.ArrayList(Secret).init(self.allocator);
        defer secrets.deinit();

        var replaced = false;
        for (parsed.value.secrets) |s| {
            if (std.mem.eql(u8, s.key, key_name)) {
                try secrets.append(.{ .key = key_name, .value = value });
                replaced = true;
            } else {
                try secrets.append(s);
            }
        }
        if (!replaced) {
            try secrets.append(.{ .key = key_name, .value = value });
        }

        const new_data = VaultData{
            .secrets = secrets.items,
            .notes = parsed.value.notes,
        };

        try self.updateData(new_data);
    }

    /// Retrieve a secret. Returns null if not found. Caller owns returned memory.
    pub fn get(self: *Vault, key_name: []const u8) !?[]u8 {
        if (self.locked) return error.VaultLocked;

        const parsed = try std.json.parseFromSlice(VaultData, self.allocator, self.data_json.?, .{});
        defer parsed.deinit();

        for (parsed.value.secrets) |s| {
            if (std.mem.eql(u8, s.key, key_name)) {
                return try self.allocator.dupe(u8, s.value);
            }
        }
        return null;
    }

    /// List all secret keys. Caller owns returned memory.
    pub fn listKeys(self: *Vault) ![][]u8 {
        if (self.locked) return error.VaultLocked;

        const parsed = try std.json.parseFromSlice(VaultData, self.allocator, self.data_json.?, .{});
        defer parsed.deinit();

        const keys = try self.allocator.alloc([]u8, parsed.value.secrets.len);
        for (parsed.value.secrets, 0..) |s, i| {
            keys[i] = try self.allocator.dupe(u8, s.key);
        }
        return keys;
    }

    /// Delete a secret by key. Returns true if found and deleted.
    pub fn delete(self: *Vault, key_name: []const u8) !bool {
        if (self.locked) return error.VaultLocked;

        const parsed = try std.json.parseFromSlice(VaultData, self.allocator, self.data_json.?, .{});
        defer parsed.deinit();

        var secrets = std.ArrayList(Secret).init(self.allocator);
        defer secrets.deinit();

        var found = false;
        for (parsed.value.secrets) |s| {
            if (std.mem.eql(u8, s.key, key_name)) {
                found = true;
            } else {
                try secrets.append(s);
            }
        }

        if (!found) return false;

        const new_data = VaultData{
            .secrets = secrets.items,
            .notes = parsed.value.notes,
        };
        try self.updateData(new_data);
        return true;
    }

    /// Add an encrypted note. Returns the new note ID.
    pub fn addNote(self: *Vault, text: []const u8) !u64 {
        if (self.locked) return error.VaultLocked;

        const parsed = try std.json.parseFromSlice(VaultData, self.allocator, self.data_json.?, .{});
        defer parsed.deinit();

        var max_id: u64 = 0;
        for (parsed.value.notes) |n| {
            if (n.id > max_id) max_id = n.id;
        }

        var notes = std.ArrayList(Note).init(self.allocator);
        defer notes.deinit();

        for (parsed.value.notes) |n| {
            try notes.append(n);
        }
        try notes.append(.{
            .id = max_id + 1,
            .text = text,
            .created_at = std.time.timestamp(),
        });

        const new_data = VaultData{
            .secrets = parsed.value.secrets,
            .notes = notes.items,
        };
        try self.updateData(new_data);
        return max_id + 1;
    }

    /// List all notes. Caller owns returned memory.
    pub fn listNotes(self: *Vault) ![]Note {
        if (self.locked) return error.VaultLocked;

        const parsed = try std.json.parseFromSlice(VaultData, self.allocator, self.data_json.?, .{});
        defer parsed.deinit();

        const notes = try self.allocator.alloc(Note, parsed.value.notes.len);
        for (parsed.value.notes, 0..) |n, i| {
            notes[i] = .{
                .id = n.id,
                .text = try self.allocator.dupe(u8, n.text),
                .created_at = n.created_at,
            };
        }
        return notes;
    }

    // -- Internal helpers --

    fn updateData(self: *Vault, data: VaultData) !void {
        const json = try std.json.stringifyAlloc(self.allocator, data, .{});
        errdefer self.allocator.free(json);

        const encrypted = try crypto.encrypt(self.allocator, json, self.key);
        defer self.allocator.free(encrypted);

        try self.saveFile(&self.salt, encrypted);

        if (self.data_json) |old| {
            @memset(old, 0);
            self.allocator.free(old);
        }
        self.data_json = json;
    }

    const FileData = struct {
        salt: [crypto.salt_len]u8,
        encrypted: []u8,
    };

    fn readFile(self: *Vault) !FileData {
        const file = try std.fs.openFileAbsolute(self.vault_path, .{});
        defer file.close();

        const all = try file.readToEndAlloc(self.allocator, 10 * 1024 * 1024);
        defer self.allocator.free(all);

        if (all.len < crypto.salt_len) return error.InvalidVaultFile;

        var salt: [crypto.salt_len]u8 = undefined;
        @memcpy(&salt, all[0..crypto.salt_len]);

        const encrypted = try self.allocator.dupe(u8, all[crypto.salt_len..]);

        return .{ .salt = salt, .encrypted = encrypted };
    }

    fn saveFile(self: *Vault, salt: *const [crypto.salt_len]u8, encrypted: []const u8) !void {
        const file = try std.fs.createFileAbsolute(self.vault_path, .{});
        defer file.close();

        try file.writeAll(salt);
        try file.writeAll(encrypted);
    }
};
