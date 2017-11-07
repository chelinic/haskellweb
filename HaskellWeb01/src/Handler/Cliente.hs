{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Cliente where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics

postClienteR :: Handler TypedContent
postClienteR = postClienteR = do
		cli <- requireJsonBody :: Handler Cliente
		cid <- runDB $  insert cli
    	sendStatusJSON created201 (object ["resp" .=(fromSqlKey cid)])

-- get404 é só por id

getBuscaClienteR :: ClienteId ->Handler Value
getBuscaClienteR cid =do
			cliente <- runDB $ get404 cid
			sendStatusJSON ok200 (object ["resp" .= toJSON cliente])
			
getEmailCliR :: Text -> Handler TypedContent
getEmailCliR email = do
            talvezCliente <- runDB $ getBy (UniqueEmail email)
            case talvezCliente of
                Nothing -> sendStatusJSON notFound404 (object ["resp" .=("ERRO" ++ show email ++ "NÃO Encontrado")])
                Just cliente -> sendStatusJSON ok200 (object ["resp" .= toJSON cliente])