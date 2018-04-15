module Infrastructure.Security

open System.Security.Cryptography
open System.IO
open System.Web.UI.WebControls

[<Literal>]
let BadPassword = "Bad password"

// Libsodium secret box port
module AuthenticatedEncryption =

    let IVSize = 16
    let MacSize = 32
    let KeySize = 32
    let AuthKeySize = 16
    let SaltSize = 16

    let private createAes key iv =
        let aes = Aes.Create()
        aes.KeySize <- KeySize * 8
        aes.BlockSize <- 128
        aes.Mode <- CipherMode.CBC
        aes.Padding <- PaddingMode.PKCS7

        aes.Key <- key
        aes.IV <- iv

        aes

    let derive (password:string) (salt:byte[]) =
        let keyDerivation = new Rfc2898DeriveBytes(password, salt)

        let key = keyDerivation.GetBytes KeySize
        let authKey = keyDerivation.GetBytes AuthKeySize
        let iv = keyDerivation.GetBytes IVSize

        authKey,iv,key

    let private rng = new System.Security.Cryptography.RNGCryptoServiceProvider()

    let encrypt (password:string) (msg:byte[]) =
        let salt = Array.zeroCreate SaltSize
        rng.GetBytes (salt)

        let (authKey,iv,key) = derive password salt

        use aes = createAes key iv
        use encrypter = aes.CreateEncryptor()
        use memoryStream = new MemoryStream()
        let cryptoStream = new CryptoStream(memoryStream, encrypter, CryptoStreamMode.Write)
        let writer = new BinaryWriter(cryptoStream)
        writer.Write(msg)
        writer.Dispose()
        cryptoStream.Dispose()

        let cipher = memoryStream.ToArray()

        use hmac = new HMACSHA256(authKey)
        let hash = hmac.ComputeHash cipher

        let (++) = Array.append

        salt ++ hash ++ cipher

    let decrypt password cipher =
        let salt,cipher = Array.splitAt SaltSize cipher
        let hash,cipher = Array.splitAt 32 cipher

        let (authKey,iv,key) = derive password salt

        use hmac = new HMACSHA256(authKey)
        if hash <> hmac.ComputeHash cipher then
            Error BadPassword
        else
            use aes = createAes key iv
            use decryptor = aes.CreateDecryptor()
            use memoryStream = new MemoryStream()
            let cryptoStream = new CryptoStream(memoryStream, decryptor, CryptoStreamMode.Write)
            let writer = new BinaryWriter(cryptoStream)
            writer.Write(cipher)
            writer.Dispose()
            cryptoStream.Dispose()

            memoryStream.ToArray() |> Ok





