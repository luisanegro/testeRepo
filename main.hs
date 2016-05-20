-- session:
    -- informação gravada no lado servidor da forma "chave" (string) "valor" (string)
    -- usada para lembrar informações do usuário, pode expirar ou não
    -- autenticação: a session "lembrará" se o usuário deu informações corretas de login e senha
-- passo 1) get (/login) usuário ok -> gravar session OU usuário não ok -> não gravar session
-- passo 2) para cada rota (que se faça necessário o usuário estar logado) basta checar a session

-- no exemplo de hoje:
    -- setSession "_ID" "3" (ID da tabela usuário)
    -- para saber quem é, basta usar o lookupSession "_ID" (Nothing OU Just)
    
-- autorização (por rota)
    -- 1) Authorized: 200 OK para a rota
    -- 2) AuthenticationRequired: 303, ou seja, redireciona para authRoute (rota definida no Yesod, que representa o login)
    -- 3) Unauthorized "msg": 400, ou seja, página de erro

-- autenticar sessão    
    -- 1) ler form
    -- SELECT * from Users
    -- where login='' and senha = ''
    
-- pack transforma String em Text
    

{-# LANGUAGE OverloadedStrings, TypeFamilies, QuasiQuotes,
             TemplateHaskell, GADTs, FlexibleInstances,
             MultiParamTypeClasses, DeriveDataTypeable,
             GeneralizedNewtypeDeriving, ViewPatterns, EmptyDataDecls #-}
import Yesod
import Database.Persist.Postgresql
import Data.Text
import Text.Lucius
import Control.Monad.Logger (runStdoutLoggingT)

data Pagina = Pagina{connPool :: ConnectionPool}

share [mkPersist sqlSettings, mkMigrate "migrateAll"] [persistLowerCase|
Users json
   nome Text
   login Text
   senha Text
   UniqueUsers login
   deriving Show
|]


mkYesod "Pagina" [parseRoutes|
-- 1
/ HomeR GET
-- 1
/login LoginR  GET POST
/erro ErroR GET
-- 1
/usuario UsuarioR GET POST
-- 1 ou 2, depende da sessão
/perfil/#UsersId PerfilR GET
-- Just "admin": 1
-- Just "_": 3
-- Nothing: 2
/admin AdminR GET
/logout LogoutR GET

|]

instance Yesod Pagina where
    authRoute _ = Just LoginR
    
    isAuthorized LoginR _ = return Authorized
    isAuthorized ErroR _ = return Authorized
    isAuthorized HomeR _ = return Authorized
    isAuthorized UsuarioR _ = return Authorized
    isAuthorized AdminR _ = isAdmin
    isAuthorized _ _ = isUser

isUser = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just _ -> Authorized
    
isAdmin = do
    mu <- lookupSession "_ID"
    return $ case mu of
        Nothing -> AuthenticationRequired
        Just "admin" -> Authorized 
        Just _ -> Unauthorized "Voce precisa ser admin para entrar aqui"

instance YesodPersist Pagina where
   type YesodPersistBackend Pagina = SqlBackend
   runDB f = do
       master <- getYesod
       let pool = connPool master
       runSqlPool f pool

type Form a = Html -> MForm Handler (FormResult a, Widget)

instance RenderMessage Pagina FormMessage where
    renderMessage _ _ = defaultFormMessage

formUser :: Form Users
formUser = renderDivs $ Users <$>
           areq textField "Nome: " Nothing <*>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

formLogin :: Form (Text,Text)
formLogin = renderDivs $ (,) <$>
           areq textField "Login: " Nothing <*>
           areq passwordField "Senha: " Nothing

getUsuarioR :: Handler Html
getUsuarioR = do
           (widget, enctype) <- generateFormPost formUser
           defaultLayout [whamlet|
                 <form method=post enctype=#{enctype} action=@{UsuarioR}>
                     ^{widget}
                     <input type="submit" value="Enviar">
           |]

getPerfilR :: UsersId -> Handler Html
getPerfilR uid = do
      user <- runDB $ get404 uid
      defaultLayout $ do
          toWidget $ $(luciusFile "templates/perfil.lucius")
          $(whamletFile "templates/perfil.hamlet")

postUsuarioR :: Handler Html
postUsuarioR = do
           ((result, _), _) <- runFormPost formUser
           case result of 
               FormSuccess user -> (runDB $ insert user) >>= \piid -> redirect (PerfilR piid)
               _ -> redirect ErroR

getHomeR :: Handler Html
getHomeR = defaultLayout [whamlet|Hello World!|]

getAdminR :: Handler Html
getAdminR = defaultLayout [whamlet|
    <h1> Bem-vindo meu Rei!
|]

getLoginR :: Handler Html
getLoginR = do
           deleteSession "_ID"
           (widget, enctype) <- generateFormPost formLogin
           defaultLayout $ $(whamletFile "templates/login.hamlet")

postLoginR :: Handler Html
postLoginR = do
           ((result, _), _) <- runFormPost formLogin
           case result of 
               FormSuccess ("admin","admin") -> setSession "_ID" "admin" >> redirect AdminR
               FormSuccess (login,senha) -> do 
                   user <- runDB $ selectFirst [UsersLogin ==. login, UsersSenha ==. senha] []
                   case user of
                       Nothing -> redirect LoginR
                       Just (Entity pid u) -> setSession "_ID" (pack $ show $ fromSqlKey pid) >> redirect (PerfilR pid)

getErroR :: Handler Html
getErroR = defaultLayout [whamlet|
     <h1> Erro de cadastro
|]

getLogoutR :: Handler Html
getLogoutR = do
     deleteSession "_ID"
     defaultLayout [whamlet| 
         <h1> ADEUS!
     |]

connStr = "dbname=d5gmjcpc2e54o2 host=ec2-54-163-240-97.compute-1.amazonaws.com user=tyhutitmwtdemt password=FXjRgaSYcz2NplDFDxctwqUyf9 port=5432"
main::IO()
main = runStdoutLoggingT $ withPostgresqlPool connStr 10 $ \pool -> liftIO $ do 
       runSqlPersistMPool (runMigration migrateAll) pool
       warp 8080 (Pagina pool)