module Frontend.Data
    ( StatsPageValues(..)
    )where

import Data.CharacterSheet

data StatsPageValues = StatsPageValues { stat_classData :: ClassData Int
                                       }

