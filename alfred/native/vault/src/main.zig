const std = @import("std");
const vault_mod = @import("vault.zig");
const Vault = vault_mod.Vault;
const Note = vault_mod.Note;

const Command = struct {
    cmd: []const u8,
    password: ?[]const u8 = null,
    key: ?[]const u8 = null,
    value: ?[]const u8 = null,
    text: ?[]const u8 = null,
};

pub fn main() !void {
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    const args = try std.process.argsAlloc(allocator);
    defer std.process.argsFree(allocator, args);

    if (args.len < 2) {
        try std.io.getStdErr().writer().writeAll("Usage: alfred-vault <vault-path>\n");
        std.process.exit(1);
    }

    var vault = Vault.init(allocator, args[1]);
    defer vault.lock();

    const stdin = std.io.getStdIn().reader();
    const stdout = std.io.getStdOut().writer();

    var buf: [65536]u8 = undefined;
    while (true) {
        const line = stdin.readUntilDelimiterOrEof(&buf, '\n') catch {
            try writeError(stdout, "read error");
            continue;
        };
        if (line == null) break;

        processCommand(allocator, &vault, line.?, stdout) catch {
            try writeError(stdout, "internal error");
        };
    }
}

fn processCommand(allocator: std.mem.Allocator, vault: *Vault, line: []const u8, writer: anytype) !void {
    const parsed = std.json.parseFromSlice(Command, allocator, line, .{}) catch {
        try writeError(writer, "invalid JSON");
        return;
    };
    defer parsed.deinit();
    const cmd = parsed.value;

    if (std.mem.eql(u8, cmd.cmd, "init")) {
        const password = cmd.password orelse {
            try writeError(writer, "password required");
            return;
        };
        vault.initVault(password) catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        try writeOkMsg(writer, "Vault initialized");
    } else if (std.mem.eql(u8, cmd.cmd, "unlock")) {
        const password = cmd.password orelse {
            try writeError(writer, "password required");
            return;
        };
        vault.unlock(password) catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        try writeOkMsg(writer, "Vault unlocked");
    } else if (std.mem.eql(u8, cmd.cmd, "lock")) {
        vault.lock();
        try writeOkMsg(writer, "Vault locked");
    } else if (std.mem.eql(u8, cmd.cmd, "store")) {
        const key = cmd.key orelse {
            try writeError(writer, "key required");
            return;
        };
        const value = cmd.value orelse {
            try writeError(writer, "value required");
            return;
        };
        vault.store(key, value) catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        try writeOkMsg(writer, "Secret stored");
    } else if (std.mem.eql(u8, cmd.cmd, "get")) {
        const key = cmd.key orelse {
            try writeError(writer, "key required");
            return;
        };
        const value = vault.get(key) catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        if (value) |v| {
            defer allocator.free(v);
            try writeOkValue(writer, v);
        } else {
            try writeError(writer, "Key not found");
        }
    } else if (std.mem.eql(u8, cmd.cmd, "list")) {
        const keys = vault.listKeys() catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        defer {
            for (keys) |k| allocator.free(k);
            allocator.free(keys);
        }
        try writeOkKeys(writer, keys);
    } else if (std.mem.eql(u8, cmd.cmd, "delete")) {
        const key = cmd.key orelse {
            try writeError(writer, "key required");
            return;
        };
        const found = vault.delete(key) catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        if (found) {
            try writeOkMsg(writer, "Secret deleted");
        } else {
            try writeError(writer, "Key not found");
        }
    } else if (std.mem.eql(u8, cmd.cmd, "note_add")) {
        const text = cmd.text orelse {
            try writeError(writer, "text required");
            return;
        };
        const id = vault.addNote(text) catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        try writeOkId(writer, id);
    } else if (std.mem.eql(u8, cmd.cmd, "notes")) {
        const notes = vault.listNotes() catch |err| {
            try writeVaultError(writer, err);
            return;
        };
        defer {
            for (notes) |n| allocator.free(@constCast(n.text));
            allocator.free(notes);
        }
        try writeOkNotes(writer, notes);
    } else {
        try writeError(writer, "Unknown command");
    }
}

// -- JSON response writers --

fn writeError(writer: anytype, msg: []const u8) !void {
    try writer.writeAll("{\"status\":\"error\",\"message\":\"");
    try writeJsonStr(writer, msg);
    try writer.writeAll("\"}\n");
}

fn writeVaultError(writer: anytype, err: anyerror) !void {
    const msg: []const u8 = if (err == error.VaultAlreadyExists)
        "Vault already exists"
    else if (err == error.VaultNotFound)
        "Vault not found"
    else if (err == error.WrongPassword)
        "Wrong password"
    else if (err == error.VaultLocked)
        "Vault is locked"
    else if (err == error.AlreadyUnlocked)
        "Already unlocked"
    else
        @errorName(err);
    try writeError(writer, msg);
}

fn writeOkMsg(writer: anytype, msg: []const u8) !void {
    try writer.writeAll("{\"status\":\"ok\",\"message\":\"");
    try writeJsonStr(writer, msg);
    try writer.writeAll("\"}\n");
}

fn writeOkValue(writer: anytype, value: []const u8) !void {
    try writer.writeAll("{\"status\":\"ok\",\"value\":\"");
    try writeJsonStr(writer, value);
    try writer.writeAll("\"}\n");
}

fn writeOkId(writer: anytype, id: u64) !void {
    try writer.print("{{\"status\":\"ok\",\"id\":{d}}}\n", .{id});
}

fn writeOkKeys(writer: anytype, keys: []const []u8) !void {
    try writer.writeAll("{\"status\":\"ok\",\"keys\":[");
    for (keys, 0..) |k, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.writeAll("\"");
        try writeJsonStr(writer, k);
        try writer.writeAll("\"");
    }
    try writer.writeAll("]}\n");
}

fn writeOkNotes(writer: anytype, notes: []const Note) !void {
    try writer.writeAll("{\"status\":\"ok\",\"notes\":[");
    for (notes, 0..) |n, i| {
        if (i > 0) try writer.writeAll(",");
        try writer.print("{{\"id\":{d},\"text\":\"", .{n.id});
        try writeJsonStr(writer, n.text);
        try writer.print("\",\"created_at\":{d}}}", .{n.created_at});
    }
    try writer.writeAll("]}\n");
}

fn writeJsonStr(writer: anytype, s: []const u8) !void {
    for (s) |c| {
        switch (c) {
            '"' => try writer.writeAll("\\\""),
            '\\' => try writer.writeAll("\\\\"),
            '\n' => try writer.writeAll("\\n"),
            '\r' => try writer.writeAll("\\r"),
            '\t' => try writer.writeAll("\\t"),
            else => {
                if (c < 0x20) {
                    try writer.print("\\u{x:0>4}", .{c});
                } else {
                    try writer.writeByte(c);
                }
            },
        }
    }
}
