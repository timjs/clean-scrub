
// 
// # Notation
//

// Clean syntax with let-before
findLocalModules :: FilePath -> Run Dictionary
findLocalModules sourceDir world
    # world = logInf ["Looking up local modules in", quote sourceDir] world
    # (result,world) = findFiles definitionPredicate sourceDir world
    | isError result = (rethrow SystemError result, world)
    # definitionPaths = fromOk result
    # moduleNames = 'List'.map translate definitionPaths
    # world = logRes ["Found local modules"] moduleNames world
    = (Ok $ 'Map'.fromList ('List'.zip2 moduleNames definitionPaths), world)
    where
        translate = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension

// Haskell syntax with do-notation
findLocalModules :: Manifest -> Run Database
findLocalModules manifest = do
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
findLocalModules :: Manifest -> Run Database
findLocalModules manifest =
    lift tell $$ Info "Searching for local modules" >>
    lift tell $$ Result "Source directory" sourceDir >>
    wrapRun $$ findFiles definitionPredicate sourceDir >>= \definitionPaths -> 
    let moduleNames = 'List'.map transform definitionPaths in
    lift tell $$ Result "Found local modules" moduleNames >>|
    'Map'.fromList $$ 'List'.zip2 moduleNames definitionPaths
    where
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension
        sourceDir = maybe "./src" id manifest.package.sources
        transform = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension

// Manual binds, with directed pipe
findLocalModules :: Manifest -> Run Database
findLocalModules manifest =
    lift tell <| Info "Searching for local modules" >>
    lift tell <| Result "Source directory" sourceDir >>
    wrapRun <| findFiles definitionPredicate sourceDir >>= \definitionPaths -> 
    let moduleNames = 'List'.map transform definitionPaths in
    lift tell <| Result "Found local modules" moduleNames >>|
    'Map'.fromList <| 'List'.zip2 moduleNames definitionPaths
    where
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension
        sourceDir = maybe "./src" id manifest.package.sources
        transform = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension

// Manual binds, with directed pipe
findLocalModules :: Manifest -> Run Database
findLocalModules manifest =
    logInfo "Searching for local modules" >>
    logResult "Source directory" sourceDir >>
    findFiles definitionPredicate sourceDir |> wrapRun >>= \definitionPaths -> 
    let moduleNames = 'List'.map transform definitionPaths in
    logResult "Found local modules" moduleNames >>|
    'List'.zip2 moduleNames definitionPaths |> 'Map'.fromList
    where
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension
        sourceDir = maybe "./src" id manifest.package.sources
        transform = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension

// Manual binds, with directed pipe
findLocalModules :: FilePath -> Run Dictionary
findLocalModules sourceDir =
    logInf ["Looking up local modules in", quote sourceDir] >>
    findFiles definitionPredicate sourceDir @ wrapRun >>= \definitionPaths ->
    let moduleNames = 'List'.map translate definitionPaths in
    logRes ["Found local modules"] moduleNames >>|
    'Map'.fromList ('List'.zip2 moduleNames definitionPaths)
    where
        translate = replace pathSeparator moduleSeparator o makeRelative sourceDir o dropExtension
        definitionPredicate info = takeExtension info.FileInformation.path == definitionExtension

createDatabase manifest world
    tell [Info "Creating main module database"]
    packages <- sequence $ traverse createPackage manifest.dependencies
    database <- findLocalModules manifest
    return $ 'List'.foldr extendDatabase database packages

createDatabase manifest world =
    logInfo ["Creating main module database"] >>
    traverse createPackage manifest.dependencies @ sequence >>= \packages ->
    findLocalModules manifest >>= \database ->
    'List'.foldr extendDatabase database packages @ return

createDatabase manifest world =
    logInfo ["Creating main module database"] >>
    traverse createPackage manifest.dependencies & sequence >>= \packages ->
    findLocalModules manifest >>= \database ->
    'List'.foldr extendDatabase database packages & return

createDatabase manifest world =
    logInfo ["Creating main module database"] >>
    traverse createPackage manifest.dependencies |> sequence >>= \packages ->
    findLocalModules manifest >>= \database ->
    'List'.foldr extendDatabase database packages |> return

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

//
// # Extras
//

showImports :: FilePath *World -> *World
showImports path world
    # world = putAct ["Calculating imports of", quote path] world
    # (result,world) = calcImports path world
    | isError result = putErr [toString $ fromError result] world
    # names = fromOk result
    = seqSt putStrLn ('Set'.toList names) world

// showDependencies path = readMainManifest >>= createMainDatabase >>= calcDependencies path >>= showDatabase
showDependencies :: FilePath *World -> *World
showDependencies path world
    # world = logAct ["Calculating dependecies of", quote path] world
    # (result,world) = readMainManifest world
    | isError result = world
    # manifest = fromOk result
    # (result,world) = createMainDatabase manifest world
    | isError result = world
    # database = fromOk result
    # (result,world) = calcDependencies path database world
    | isError result = putErr [toString $ fromError result] world
    # names = fromOk result
    = seqSt putStrLn ('Set'.toList names) world

/// ## Casting

/// Class to cast arbitrary types to each other
class Cast a b where
    cast :: a -> b

instance Cast OSError Error
instance Cast FileError Error
// instance Cast (Either e a) (Either e` a) | Cast e e`

instance Cast OSError Error where
    cast e = SystemError e
instance Cast FileError Error where
    cast e = FileError e
// instance Cast (Either e a) (Either e` a) | Cast e e` where
//     cast (Left e) = Left (cast e)
//     cast (Right a) = Right a

////////////////////////////////////////////////////////////////////////////////
/// # Module dictionary
////////////////////////////////////////////////////////////////////////////////

mkDatabase :: [Package] -> Dictionary
mkDatabase packages
    = foldMap extractDatabase packages //XXX add duplication check of modules

extractDatabase :: Package -> Dictionary
extractDatabase package
    = 'Map'.fromList $ 'List'.zip2 names paths
    where ...

extendDatabase :: Package Dictionary -> Dictionary
extendDatabase package dictionary
    // traceAct ["Extending module dictionary with", quote package.manifest.info.BasicInfo.name] $
    = 'List'.foldr (uncurry 'Map'.insert) dictionary $ 'List'.zip2 moduleNames definitionPaths
    where
        moduleNames = maybe [] (\info -> info.modules) package.manifest.library
        definitionPaths = 'List'.map transform moduleNames
        transform name = package.Package.path </> maybe "" id package.manifest.info.sources </> replace moduleSeparator pathSeparator name <.> definitionExtension
        //XXX someday: transform name = scrubPackageRoot </> package.name </> package.version </> package.sources </> replace moduleSeparator pathSeparator name <.> definitionExtension

