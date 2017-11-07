{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TypeFamilies #-}
module Handler.Pagina where

import Import
import Text.Julius

-- não fechar as tags 
-- o haskell fecha automaticamente
-- pode fechar tag inline

getPag1R :: Handler Html
getPag1R = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Pagina 1
            <a href=@{HomeR}> Voltar
    |]

getPag2R :: Handler Html
getPag1R = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Pagina 1
            <a href=@{HomeR}> Voltar
        |]

getPag3R :: Handler Html
getPag1R = do
    defaultLayout $ do
        toWidget $ [lucius|
            h1{
                color:pink;
            }
        |]
        [whamlet|
            <h1> Pagina 1
            <a href=@{HomeR}> Voltar
        |]

getHomeR :: Handler Html
getHomeR = do
        defaultLayout $ do
        toWidgetHead $ $(juliusFile "templates/home.julius")
        addStylesheet $ (StaticR css_home_css)
        addStylesheet $ (StaticR css_bootstrap_css)
        $ (whamletFile "templates/home.whamlet")
        
soma :: Int -> Int -> Int
soma x y = x + y

getAddR :: Int -> Int -> Handler Html
getAddR x y do
    defaultLayout $ do
    [whamlet|
        <h1> A soma deu: #{soma x y}
    |]


paginaDentro :: Widget
paginaDentro = do
    toWidget $ [cassius|
        h1
            color:orange;
    |]
    [whamlet|
        <h1>Ola Mundo
    |]
    
safeTail :: [a] -> Maybe [a]
safeTail [] = Nothing
safeTail (x:[]) = Nothing
safeTail (_:xs) = Just xs

getTailR :: Text -> Handler Html
getTailR palavra = do
    palstring <- return $ unpack palavra
    t <- return $ safeTail palstring -- Handler (Maybe [a])
    defaultLayout $ do
        [whamlet|
            $maybe jt <-t
                <h1> O Tail eh: #{jt}
            $Nothing
                <h1> Deu ruim aqui 
        |]

-- unpack retorna string 
-- return para colocar algo puro dentro da mônada
-- $ para monada Maybe


-- lista de itens
getExemploR :: Handler Html
getExemploR = do
    defaultLayout $ do
    addStylesheet $ (StaticR css_home_css)
    [whamlet|
        <div class="container">
            ^{paginaDentro}
    |]
    
getListR :: Handler Html
getListR = do
    lista <- return $ ["Santos","Gremio","Palmeiras","Cruzeiro","Botafogo","Flamengo"] :: Handler [String]
    defaultLayout $ do
    [whamlet|
        <ul>
            $forall time <- lista
                <li> #{pack time}
    |]
    
    