{-# LANGUAGE OverloadedStrings, KindSignatures, DataKinds, GADTs, MultiWayIf #-}

module Tricks.PhantomType (
   User,
   newUser,
   login,
   withUser
) where


import Data.Set as S
import Data.Text



data Status = Login | Logoff

data UserInfo = UserInfo { _name :: Text } deriving (Show)

data User (s :: Status) where
   User :: UserInfo -> User s


newUser :: UserInfo -> User Logoff
newUser = User

login :: User a -> Maybe (User Login)
login (User info) =
   let users = S.fromList ["a", "b"] -- Replace this by a set of allowed users
   in if | _name info `member` users -> return $ User info
         | otherwise                 -> Nothing

withUser :: Monad m => User Login -> (UserInfo -> m a) -> m a
withUser (User info) f = f info
