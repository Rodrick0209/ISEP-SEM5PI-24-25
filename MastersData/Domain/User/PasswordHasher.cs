using System;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Identity;
using System.Security.Cryptography;

namespace DDDSample1.Domain.User
{
    public class PasswordHasher 
    {

        private const int SaltSize = 128/8;
        private const int KeySize = 256/8;
        private const int Iterations = 10000;
        private static readonly HashAlgorithmName _hashAlgorithm = HashAlgorithmName.SHA256;
        private static char Delimiter = ';';    

        public string passwordHash { get; private set; }

        public string HashPassword(string passwordHash)
        { 
            var salt = RandomNumberGenerator.GetBytes(SaltSize);
            var hash = Rfc2898DeriveBytes.Pbkdf2(passwordHash, salt, Iterations,_hashAlgorithm,KeySize);
            return string.Join(Delimiter,Convert.ToBase64String(salt), Convert.ToBase64String(hash));
        }


        public bool VerifyPassword(string hashedPassword, string inputPassword)
        {
            var elements = hashedPassword.Split(Delimiter);
            var salt = Convert.FromBase64String(elements[0]);
            var hash = Convert.FromBase64String(elements[1]); 

            var hashInput = Rfc2898DeriveBytes.Pbkdf2(inputPassword, salt, Iterations,_hashAlgorithm,KeySize); 
            return CryptographicOperations.FixedTimeEquals(hash, hashInput);  
        }
    }
}
