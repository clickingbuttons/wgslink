const std = @import("std");
const Ast = @import("./Ast.zig");
const Data = @import("./Node.zig");

const Node = Data.Node;

pub fn Visitor(comptime RetType: type) type {
    return struct {
        pub const VTable = struct {
            visitSpan: *const fn (*Self, Node.Span) RetType,
            visitGlobalVar: *const fn (*Self, Node.GlobalVar) RetType,
            visitType: *const fn (*Self, Node.Ident) RetType,
            visitNumber: *const fn (*Self, Node.Number) RetType,
            visitIdent: *const fn (*Self, Node.Ident) RetType,
        };

        const Self = @This();

        ptr: *anyopaque,
        vtable: *const VTable,
        tree: *Ast,

        pub fn visit(self: *Self, node: Node) RetType {
            return switch (node) {
                .span => |n| self.vtable.visitSpan(self, n),
                .global_var => |n| self.vtable.visitGlobalVar(self, n),
                .type => |n| self.vtable.visitType(self, n),
                .number => |n| self.vtable.visitNumber(self, n),
                .ident => |n| self.vtable.visitIdent(self, n),
                else => {
                    std.debug.print("unknown node type {s}\n", .{@tagName(node)});
                },
            };
        }

        pub fn visitIndex(self: *Self, index: Node.Index) RetType {
            if (index == 0) return;
            return self.visit(self.tree.node(index));
        }
    };
}
