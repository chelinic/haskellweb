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
        
        
-- pode fechar tag inline

soma :: Int -> Int -> Int
soma x y = x + y

getAddR :: Int -> Int -> Handler Html
getAddR x y do
    defaultLayout $ do
    [whamlet|
        <h1> A soma deu: #{soma x y}
    |]

