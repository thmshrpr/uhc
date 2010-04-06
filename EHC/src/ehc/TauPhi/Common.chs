%%[0
%include lhs2TeX.fmt
%include afp.fmt
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% TauPhi Common
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[8 module {%{EH}TauPhi.Common} import({%{EH}Base.Common})
%%]

%%[(8 tauphi) hs export(Strictness(..), Uniqueness(..))
%%]

%%[20 import(Control.Monad, {%{EH}Base.Binary}, {%{EH}Base.Serialize})
%%]

%%[(8 tauphi)
data Strictness
  = Strict
  | NonStrict
  | StrictnessVar HsName
  deriving (Eq, Ord)

instance Show Strictness where
  show Strict            = "strict"
  show NonStrict         = "nonStrict"
  show (StrictnessVar n) = "strictness:" ++ show n

data Uniqueness
  = Unique
  | NonUnique
  | UniquenessVar HsName
  deriving (Eq, Ord)

instance Show Uniqueness where
  show Unique            = "unique"
  show NonUnique         = "nonUnique"
  show (UniquenessVar n) = "uniqueness:" ++ show n

%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Typeable, Data
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 tauphi)
deriving instance Typeable Strictness
deriving instance Data Strictness

deriving instance Typeable Uniqueness
deriving instance Data Uniqueness
%%]

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Instances: Binary, Serialize
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%[(20 tauphi)
instance Serialize Strictness where
  sput (Strict)    = sputWord8 0
  sput (NonStrict) = sputWord8 1
  sput (Var nm)    = sputWord8 2 >> sput nm
  sget = do t <- sgetWord8
            case t of
              0 -> return Strict
              1 -> return NonStrict
              2 -> liftM StrictnessVar sget

instance Serialize Uniqueness where
  sput (Unique)           = sputWord8 0
  sput (NonUnique)        = sputWord8 1
  sput (UniquenessVar nm) = sputWord8 2 >> sput nm
  sget = do t <- sgetWord8
            case t of
              0 -> return Unique
              1 -> return NonUniqe
              2 -> liftM UniquenessVar sget
%%]
