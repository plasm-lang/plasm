use hir::{HIRType, THIR};
use serde::de::value;
use utils::ids::{TypeId, ValueId};
use utils::primitive_types::PrimitiveType;

use super::mir::{
    BasicBlock, Constant, Function, Instruction, InternalFunction, MIR, Module, Operand, RValue,
    Terminator,
};

pub fn hir_to_mir(hir: THIR) -> MIR {
    let translator = HIRTranslator::new();
    translator.translate(hir)
}

struct HIRTranslator {
    mir: MIR,
}

impl HIRTranslator {
    fn new() -> Self {
        HIRTranslator {
            mir: MIR {
                modules: vec![Module::default()],
            },
        }
    }

    fn translate(mut self, hir: THIR) -> MIR {
        for item in hir.items {
            match item {
                hir::Item::Function(func) => {
                    let func = self.translate_function(func);
                    self.mir.modules[0].functions.push(func);
                }
            }
        }
        self.mir
    }

    fn translate_function(&mut self, func: hir::TypedFunction) -> Function {
        // For beggining, only support functions with no arguments and void return type
        assert!(func.signature.args.is_empty());
        assert!(func.signature.ret_ty.node == HIRType::Primitive(PrimitiveType::Void));

        if func.body.statements.is_empty() {
            return Function::Internal(InternalFunction {
                name: func.signature.name.node.clone(),
                blocks: vec![],
            });
        }

        let mut instructions = Vec::new();

        let mut current = ValueId::one();
        let mut new_value_id = || {
            let id = current;
            current = current.increment();
            id
        };

        for statement in func.body.statements {
            match statement {
                hir::Statement::VariableDeclaration(decl) => {
                    // Alloca for stack variable
                    let type_id = TypeId::one(); // Placeholder, type handling not implemented yet
                    let stack_ptr = new_value_id();
                    let rvalue = RValue::Alloca(type_id);
                    let instruction = Instruction::Assign(stack_ptr, rvalue);
                    instructions.push(instruction);

                    // Store value in virtual register
                    // !!! Placeholder for initializing constant value for now
                    // let value_id = new_value_id();
                    // let rvalue = RValue::Constant(Constant {});
                    // let instruction = Instruction::Assign(value_id, rvalue);
                    // instructions.push(instruction);

                    // Store instruction
                    let instruction = Instruction::Store {
                        value: Operand::Constant(Constant {}),
                        ptr: Operand::Use(stack_ptr),
                    };
                    instructions.push(instruction);
                }
                _ => unimplemented!(),
            }
        }

        let entry_block = BasicBlock {
            label: "entry".to_string(),
            phis: vec![],
            instructions,
            terminator: Terminator::Return(Operand::Constant(Constant {})),
        };

        Function::Internal(InternalFunction {
            name: func.signature.name.node.clone(),
            blocks: vec![entry_block],
        })
    }
}
