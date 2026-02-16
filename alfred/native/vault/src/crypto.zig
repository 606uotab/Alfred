const std = @import("std");
const Sha256 = std.crypto.hash.sha2.Sha256;
const Aes256Gcm = std.crypto.aead.aes_gcm.Aes256Gcm;

pub const key_len = Aes256Gcm.key_length;
pub const nonce_len = Aes256Gcm.nonce_length;
pub const tag_len = Aes256Gcm.tag_length;
pub const salt_len = 32;

const kdf_iterations = 100_000;

/// Derive a 32-byte AES key from password + salt via iterated SHA-256.
pub fn deriveKey(password: []const u8, salt: *const [salt_len]u8) [key_len]u8 {
    var h = Sha256.init(.{});
    h.update(password);
    h.update(salt);
    var key = h.finalResult();

    for (0..kdf_iterations) |_| {
        h = Sha256.init(.{});
        h.update(&key);
        h.update(password);
        key = h.finalResult();
    }

    return key;
}

/// Generate a cryptographically secure random salt.
pub fn generateSalt() [salt_len]u8 {
    var salt: [salt_len]u8 = undefined;
    std.crypto.random.bytes(&salt);
    return salt;
}

/// Encrypt plaintext with AES-256-GCM.
/// Returns allocated buffer: nonce(12) || tag(16) || ciphertext.
pub fn encrypt(allocator: std.mem.Allocator, plaintext: []const u8, key: [key_len]u8) ![]u8 {
    var nonce: [nonce_len]u8 = undefined;
    std.crypto.random.bytes(&nonce);

    const out = try allocator.alloc(u8, nonce_len + tag_len + plaintext.len);
    errdefer allocator.free(out);

    @memcpy(out[0..nonce_len], &nonce);

    var tag: [tag_len]u8 = undefined;
    Aes256Gcm.encrypt(
        out[nonce_len + tag_len ..],
        &tag,
        plaintext,
        "",
        nonce,
        key,
    );

    @memcpy(out[nonce_len .. nonce_len + tag_len], &tag);

    return out;
}

/// Decrypt a buffer (nonce || tag || ciphertext) with AES-256-GCM.
/// Returns allocated plaintext.
pub fn decrypt(allocator: std.mem.Allocator, data: []const u8, key: [key_len]u8) ![]u8 {
    if (data.len < nonce_len + tag_len) return error.InvalidData;

    const nonce: [nonce_len]u8 = data[0..nonce_len].*;
    const tag: [tag_len]u8 = data[nonce_len .. nonce_len + tag_len].*;
    const ciphertext = data[nonce_len + tag_len ..];

    const plaintext = try allocator.alloc(u8, ciphertext.len);
    errdefer allocator.free(plaintext);

    Aes256Gcm.decrypt(
        plaintext,
        ciphertext,
        tag,
        "",
        nonce,
        key,
    ) catch return error.DecryptionFailed;

    return plaintext;
}
