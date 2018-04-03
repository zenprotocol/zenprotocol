module Infrastructure.Crypto

module SecretBox = 
    open System.Security.Cryptography
    open Org.BouncyCastle.Security

    [<Literal>]
    let ivBytes = 16

    let private createAes key iv =
        let aes = Aes.Create()
        aes.Key <- key
        aes.IV <- iv
        //TODO: fine-tuning
        // aes.KeySize
        // aes.LegalKeySizes
        // aes.LegalBlockSizes
        // aes.BlockSize
        aes.Mode <- CipherMode.CBC 
        aes.Padding <- PaddingMode.None //TODO: fine-tuning
        aes
    
    let private transform value (transform:ICryptoTransform) =
        transform.TransformFinalBlock (value, 0, Array.length value)
    
    let create value key iv =
        try 
            use aes = createAes key iv 
            aes.CreateEncryptor()
            |> transform value
            |> Ok
        with _ as ex ->
            Error ex.Message 
            
    let openBox value key iv =
        try
            use aes = createAes key iv 
            aes.CreateDecryptor()
            |> transform value
            |> Ok
        with _ as ex ->
            Error ex.Message 

    let generateIV =
        let rng = new SecureRandom()
        rng.GenerateSeed ivBytes

module KeyDerivation =
    open Org.BouncyCastle.Crypto.Generators
    open Org.BouncyCastle.Crypto.Parameters
    open System.Text

    //TODO: fine-tuning
    [<Literal>]
    let seedBytes = 20
    [<Literal>]
    let hashBytes = 20
    [<Literal>]
    let iterations = 1000
    
    let deriveKey password salt = 
        let kdf = new Pkcs5S2ParametersGenerator()
        kdf.Init(Encoding.UTF8.GetBytes (password : string), salt, iterations)
        (kdf.GenerateDerivedMacParameters (8*hashBytes) :?> KeyParameter).GetKey()