use super::bytecode::*;
use crate::scanner::{
    position::Located,
    ast::*
};
use std::collections::HashMap;

pub struct Compiler {
    pub frames: Vec<CompilerFrame>,
}
#[derive(Debug, Clone, PartialEq)]
pub struct CompilerFrame {
    closure: Closure,
    registers: Register,
    scopes: Vec<Scope>,
}
#[derive(Debug, Clone, PartialEq, Default)]
pub struct Scope {
    pub locals: HashMap<String, Register>,
    pub offset: Register,
}

impl CompilerFrame {
    pub fn scope(&self) -> Option<&Scope> {
        self.scopes.last()
    }
    pub fn scope_mut(&mut self) -> Option<&mut Scope> {
        self.scopes.last_mut()
    }
    pub fn push_scope(&mut self) {
        self.scopes.push(Scope {
            offset: self.registers,
            ..Default::default()
        })
    }
    pub fn push_scope_params(&mut self, params: Vec<String>) {
        let offset = self.registers;
        let mut locals = HashMap::new();
        for param in params {
            let register = self.new_register();
            locals.insert(param, register);
        }
        self.scopes.push(Scope { offset, locals })
    }
    pub fn pop_scope(&mut self) {
        if let Some(scope) = self.scopes.pop() {
            self.registers = scope.offset;
        }
    }
    pub fn new_register(&mut self) -> Register {
        self.registers += 1;
        if self.registers > self.closure.registers {
            self.closure.registers = self.registers;
        }
        self.registers
    }
    pub fn new_local(&mut self, ident: String) -> Register {
        let register = self.new_register();
        let scope = self.scope_mut().expect("no scope");
        scope.locals.insert(ident, register);
        register
    }
    pub fn write(&mut self, bytecode: ByteCode, ln: u32) {
        self.closure
            .code
            .push(ByteCodeLocated { code: bytecode, ln })
    }
    pub fn overwrite(&mut self, addr: Address, bytecode: ByteCode) {
        if let Some(code) = self.closure.code.get_mut(addr as usize) {
            code.code = bytecode;
        }
    }
}

#[derive(Debug, Clone)]
pub enum CompileError {
    NoLocal(String),
}
pub trait Compilable {
    type Output;
    fn compile(compiler: &mut Compiler) -> Result<Self::Output, Located<CompileError>>;
}
