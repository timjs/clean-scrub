
// 
// # Notation
//

// Haskell syntax with do-notation
localModules :: Manifest -> Run Database
localModules manifest = do
    tell $ Info "Searching for local modules"
    tell $ Result "Source directory" sourceDir
    definitionPaths <- wrapRun $ findFiles definitionPredicate sourceDir
    let moduleNames = 'List'.map transform definitionPaths
    tell $ Result "Found local modules" moduleNames
    return $ 'Map'.fromList $ 'List'.zip2 moduleNames definitionPaths
    where
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension
        sourceDir = maybe "./src" id manifest.package.sources
        transform = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension

// Manual binds, with stronger dollar
localModules :: Manifest -> Run Database
localModules manifest =
    lift tell $$ Info "Searching for local modules" >>
    lift tell $$ Result "Source directory" sourceDir >>
    wrapRun $$ findFiles definitionPredicate sourceDir >>= \definitionPaths -> 
    let moduleNames = 'List'.map transform definitionPaths in
    lift tell $$ Result "Found local modules" moduleNames >>
    return $$ 'Map'.fromList $$ 'List'.zip2 moduleNames definitionPaths
    where
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension
        sourceDir = maybe "./src" id manifest.package.sources
        transform = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension

//
// # Ideas
//

(`catch`) infixl 9 :: (Result e a) (e -> Result e` a) -> Result e` a

(`catch`) infixl 9 :: (Result e a) (e -> Result e` a) -> Result e` a
(`catch`) (Error e) handler = handler e
(`catch`) (Ok a) _ = Ok a

// :: Message
//     = Err String
//     | Wrn String
//     | Inf String
//     | Act String
//     | Res String String

