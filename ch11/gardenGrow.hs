data FlowerType = Gardenia
                  | Daisy
                  | Rose
                  | Lilac
                  deriving Show

type Gardener = String

data Garden = Garden Gardener FlowerType
  deriving Show

data Garden2 = Gardenia2 Gardener
              | Daisy2 Gardener
              | Rose2 Gardener 
              | Lilac2 Gardener
              deriving Show