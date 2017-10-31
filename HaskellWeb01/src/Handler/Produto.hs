{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Home where

import Import
import Network.HTTP.Types.Status
import Database.Persist.Postgresql
import GHC.Generics
	
	data Nome = Nome {nome :: Text} deriving Generic
	instance ToJSON Nome where
	instance FromJSON Nome where

getHomeR :: Handler Html
getHomeR = undefined

postProdutoR :: Handler TypedContent
	postProdutoR = do
		produt <- requireJsonBody :: Handler Produto
		pid <- runDB $  insert prod
    	sendStatusJSON created201 (object ["resp" .=(fromSqlKey pid)])
		-- "retornar um 201 para dizer que houve exito"
		-- object transforma {"resp" :1} em JSON
				-- o produto com id 1

deleteProdutoDelR :: ProdutoId ->Handler Value
	deleteProdutoDelR pid = do
	        _ <- runDB $ get404 pid
	    	runDB $ delete pid
		    sendStatusJSON noContent204 (object ["resp" .=("Deleted" ++ show (fromSqlKey pid))])
	{-- o get404 procura o registro e prossegue caso encontre 
	 barra o restante da aplicação e manda um 404
	--}
	
getBuscaProdutoR :: ProdutoId ->Handler Value
getBuscaProdutoR pid =do
			produto <- runDB $ get404 pid
			sendStatusJSON ok200 (object ["resp" .= toJSON produto])
	
putAlteraProdR :: ProdutoId ->Handler Value
putAlteraProdR pid = do
			_ <- runDB $ get404 pid
			novoProd <- requireJsonBody :: Handler Produto
			runDB $ replace pid novoProd
			sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey pid))])
	
patchAlteraNomeR :: ProdutoId ->Handler Text
patchAlteraNomeR pid = do
			_ <- runDB $ get404 pid		
			novoNome <- requireJsonBody :: Handler Produto
			runBD $ update pid [ProdutoNome =. (nome novoNome)]
			sendStatusJSON noContent204 (object ["resp" .=("Updated" ++ show (fromSqlKey pid))])