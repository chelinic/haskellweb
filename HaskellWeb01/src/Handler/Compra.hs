{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}

module Handler.Compra where

import Import
import Control.Monad
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics

postCompraR :: Handler TypedContent
postCompraR = do
		compra <- requireJsonBody :: Handler Compra
		_ <- runDB $ get404 $ compraCliid compra
		_ <- runDB $ get404 $ compraProid compra
		compid <- runDB $ insert compra
    	sendStatusJSON created201 (object ["resp" .=(fromSqlKey compid)])
    	
    	
third :: (a,b,c) -> a
third (a,b,c) = a
-- é necessário para a tupla com 3 entities para pegar apenas o produto

getListaProdR :: ClienteId -> Handler TypedContent
getListaProdR cid = do
        liprod <- runDB $ rawSql
        ("SELECT ?? , ?? , ??\
        FROM produto INNER JOIN compra \
        ON produto.id = compra.proid INNER JOIN cliente\
        ON compra.cliid = cliente.id\
        WHERE cliente.id = " <> pack $ fromSqlKey cid)
        [] :: Handler [(Entity Produto, Entity Compra, Entity Cliente)]
        produtos <- fmap third lista
        produtossemid <- return fmap (\(Entity _ prod) -> prod) produtos
        -- produtos sem o id para mostar o nome apenas
        sendStatusJSON ok200 (object ["resp" .= (toJSON produtossemid))])
        
-- pack transforma para text e se usa mappend ao invés de ++

getListaProdFR :: ClienteId -> Handler TypedContent
getListaProdR cid = do
        lista <- selectList [CompraCliid ==. cid] []
            -- tipo :: Handler [Entity Compra]
        listasemid <- return fmap (\(Entity _ comp) -> comp) lista
            -- tipo :: Handler [Compra]
            -- tira o Entity (tabela inteira)
        prodsIds <- return $ fmap compraProid lista
            -- tipo :: Handler [Compraid] == Handler [Int]
        produtos <- sequence $ fmap (\pid -> runDB $ get404 pid) prodsIds
            -- (após o sequence) Troca os inteiros por produtos :: [Handler Produto]    
            -- o sequence faz as monads Handler[Compra] com [Handler Produto]
            -- sequence :: Handler[Produto]
            -- sequence :: Monad m => [m a] -> m[a]
        sendStatusJSON ok200 (object ["resp" .= (toJSON produtos))])
        
        --o objetivo é ter um handler de listas e não uma lista de handlers
        -- o sequence executa o fmap para todos os Ids de produto
        