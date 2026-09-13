#[derive(Debug, Clone, PartialEq, Copy)]
pub enum Annotations {
    N = 0,
    V = 1,
    S = 2,
    X = 3,
}

impl Annotations {
    pub fn annotation_from_code(code: u8) -> Annotations {
        match code {
            0 => Annotations::N,
            1 => Annotations::V,
            2 => Annotations::S,
            3 => Annotations::X,
            _ => panic!("Unsupported annotation code: {}", code),
        }
    }
    pub fn to_vec_of_annot(annot_vector: Vec<u8>) -> Vec<Annotations> {
        annot_vector
            .into_iter()
            .map(Annotations::annotation_from_code)
            .collect()
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum VarType {
    Var1i,
    Var2,
    VarNNi,
    Var1iD,
    Var1iA,
    Var2D,
    Var2A,
    VarNNiD,
    VarNNiA,
}
