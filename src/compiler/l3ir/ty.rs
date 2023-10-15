use anyhow::{anyhow, bail, Result};
use spirv::BuiltIn;
use std::{collections::{HashMap, hash_map::Entry}, rc::Rc};

macro_rules! def_into_type_ref {
    ($($name:ident,)+) => {
        impl std::fmt::Debug for Type {
            fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
                match self {
                    $(
                        Type::$name(x) => x.fmt(f),
                    )+
                }
            }
        }
        $(
            paste::paste! {
                impl [<Type $name>] {
                    pub fn into_type_ref(self) -> TypeRef {
                        TypeRef::new(Type::$name(self))
                    }
                }
                impl TypeRef {
                    #[allow(dead_code)]
                    pub fn [<as_ $name:lower>](&self) -> Option<&[<Type $name>]> {
                        match self.as_ref() {
                            Type::$name(x) => Some(x),
                            _ => None,
                        }
                    }
                    #[allow(dead_code)]
                    pub fn [<is_ $name:lower>](&self) -> bool {
                        self.[<as_ $name:lower>]().is_some()
                    }
                }
            }
        )+
    };
}

macro_rules! adt_fields {
    ($($name:literal => $t:expr,)+) => {
        vec![
            $(
                ($name.to_string(), $t),
            )+
        ]
    };
}
macro_rules! adt_generics {
    ($($name:literal => $t:expr,)+) => {
        adt_fields!($($name => $t,)+).into_iter().collect::<HashMap<_, _>>()
    };
}

#[derive(Debug, Clone)]
pub struct TypeRef {
    inner: Rc<Type>,
}
impl TypeRef {
    pub fn new(ty: Type) -> Self {
        Self {
            inner: Rc::new(ty),
        }
    }
    pub fn as_ref(&self) -> &Type {
        self.inner.as_ref()
    }
    pub fn as_ptr(&self) -> *const Type {
        self.inner.as_ref() as *const Type
    }
}
impl std::hash::Hash for TypeRef {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.as_ptr().hash(state)
    }
}
impl PartialEq for TypeRef {
    fn eq(&self, other: &Self) -> bool {
        std::ptr::eq(self.as_ptr(), other.as_ptr())
    }
}
impl Eq for TypeRef {}

#[derive(Debug, Clone)]
pub struct TypeVoid {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeBool {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeInt {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeFloat {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeInstr {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeIdRef {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeBlock {
    name: String,
}
#[derive(Debug, Clone)]
pub struct TypeAdt {
    name: String,
    // Any shallow occursion of Generic type here is always transcribed in
    // `generic_args`.
    fields: Vec<(String, TypeRef)>,
}
#[derive(Debug, Clone)]
pub struct TypeGeneric {
    name: String,
}

#[derive(Clone)]
pub enum Type {
    Void(TypeVoid),
    Bool(TypeBool),
    Int(TypeInt),
    Float(TypeFloat),
    Instr(TypeInstr),
    IdRef(TypeIdRef),
    Block(TypeBlock),
    Adt(TypeAdt),
    Generic(TypeGeneric),
}
def_into_type_ref!(
    Void,
    Bool,
    Int,
    Float,
    Instr,
    IdRef,
    Block,
    Adt,
    Generic,
);
impl TypeRef {
    pub fn name(&self) -> &str {
        match self.as_ref() {
            Type::Void(x) => &x.name,
            Type::Bool(x) => &x.name,
            Type::Int(x) => &x.name,
            Type::Float(x) => &x.name,
            Type::Instr(x) => &x.name,
            Type::IdRef(x) => &x.name,
            Type::Block(x) => &x.name,
            Type::Adt(x) => &x.name,
            Type::Generic(x) => &x.name,
        }
    }

    // Note that `generic_args` may contain generic types for partial
    // specialization.
    pub fn specialize(&self, generic_args: &HashMap<String, TypeRef>) -> Result<TypeRef> {
        let out = match self.as_ref() {
            Type::Adt(x) => {
                let mut fields = vec![];
                for (field_name, field_ty) in x.fields.iter() {
                    let field_ty = field_ty.specialize(generic_args)?;
                    fields.push((field_name.clone(), field_ty));
                }

                let out = TypeAdt {
                    name: x.name.clone(),
                    fields,
                }.into_type_ref();

                out
            }
            Type::Generic(x) => {
                let generic_name = &x.name;
                let arg_ty = generic_args.get(generic_name)
                    .ok_or_else(|| anyhow!("generic type {} is not specified", generic_name))?;

                arg_ty.clone()
            }
            _ => self.clone(),
        };

        Ok(out)
    }
}

#[derive(Debug, Clone)]
struct TypeRegistry {
    types: HashMap<String, TypeRef>,
    void_ty: TypeRef,
    bool_ty: TypeRef,
    int_ty: TypeRef,
    float_ty: TypeRef,
    instr_ty: TypeRef,
    idref_ty: TypeRef,
    block_ty: TypeRef,
}
impl TypeRegistry {
    pub fn register(&mut self, ty: TypeRef) -> Result<TypeRef> {
        let ty_name = ty.name();
        match self.types.entry(ty_name.to_string()) {
            Entry::Occupied(_) => {
                bail!("type {} is already defined", ty_name);
            }
            Entry::Vacant(e) => {
                let out = e.insert(ty).clone();
                Ok(out)
            },
        }
    }

    pub fn new() -> Result<Self> {
        // Void type.
        let void_ty = TypeVoid {
            name: "__Void".to_string(),
        }.into_type_ref();

        // Bool type.
        let bool_ty = TypeBool {
            name: "__Bool".to_string(),
        }.into_type_ref();

        // Int type.
        let int_ty = TypeInt {
            name: "__Int".to_string(),
        }.into_type_ref();

        // Float type.
        let float_ty = TypeFloat {
            name: "__Float".to_string(),
        }.into_type_ref();

        // Instr type.
        let instr_ty = TypeInstr {
            name: "__Instr".to_string(),
        }.into_type_ref();

        // IdRef type.
        let idref_ty = TypeIdRef {
            name: "__IdRef".to_string(),
        }.into_type_ref();

        // Block type.
        let block_ty = TypeBlock {
            name: "__Block".to_string(),
        }.into_type_ref();

        let out = Self {
            types: HashMap::new(),
            void_ty,
            bool_ty,
            int_ty,
            float_ty,
            instr_ty,
            idref_ty,
            block_ty,
        };
        Ok(out)
    }
}
