using System;
using System.Security.Claims;
using System.Text;
using DDDSample1.Domain.User;
using Microsoft.Extensions.Configuration;
using Microsoft.IdentityModel.JsonWebTokens;
using Microsoft.IdentityModel.Tokens;

namespace DDDSample1.Infrastructure.Users
{
    public sealed class TokenProvider(IConfiguration configuration)
    {
        public string Create(User user)
        {
            string secretKey = configuration["Jwt:SecretKey"];
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));

            var credentials = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);
            double expirationTime = Convert.ToDouble(configuration["Jwt:ExpirationTime"]);


            var tokerDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(
                [
                    new Claim(JwtRegisteredClaimNames.Sub,user.Id.ToString()),
                    new Claim(JwtRegisteredClaimNames.Email,user.email.email),
                    new Claim("role",user.role.role),
                ]),
                Expires = DateTime.UtcNow.AddMinutes(expirationTime),
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                Audience = configuration["Jwt:Audience"]

            };

            var handler = new JsonWebTokenHandler();

            string token = handler.CreateToken(tokerDescriptor);

            return token;


        }

        public string CreateTokenForGoogle(string email)
        {
            string secretKey = configuration["Jwt:SecretKey"];
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));

            var credentials = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);
            double expirationTime = Convert.ToDouble(configuration["Jwt:ExpirationTime"]);


            var tokerDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(
                [
                        new Claim(JwtRegisteredClaimNames.Sub, email),  // Opcionalmente use um UserId aqui
                        new Claim(JwtRegisteredClaimNames.Email, email)                ]),
                Expires = DateTime.UtcNow.AddMinutes(expirationTime),
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                Audience = configuration["Jwt:Audience"]

            };

            var handler = new JsonWebTokenHandler();

            string token = handler.CreateToken(tokerDescriptor);

            return token;


        }









        public string CreatePasswordResetToken(User user)
        {
            string secretKey = configuration["Jwt:SecretKey"];
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
            var credentials = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);

            var tokenDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(new[]
                {
                    new Claim(JwtRegisteredClaimNames.Sub, user.Id.ToString()),
                    new Claim(JwtRegisteredClaimNames.Email, user.email.email),
                    new Claim("purpose", "password_reset")
                }),
                Expires = DateTime.UtcNow.AddMinutes(2),
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                Audience = configuration["Jwt:Audience"]
            };

            var handler = new JsonWebTokenHandler();
            string token = handler.CreateToken(tokenDescriptor);

            return token;
        }

        public string CreateConfirmationRegisterPatientToken(User user)
        {
            string secretKey = configuration["Jwt:SecretKey"];
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
            var credentials = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);

            var tokenDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(new[]
                {
                    new Claim(JwtRegisteredClaimNames.Sub, user.Id.ToString()),
                    new Claim(JwtRegisteredClaimNames.Email, user.email.email),
                    new Claim("purpose", "confirmation_register_patient")
                }),
                Expires = DateTime.UtcNow.AddMinutes(2), // Expira em 24 horas
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                Audience = configuration["Jwt:Audience"]
            };

            var handler = new JsonWebTokenHandler();
            string token = handler.CreateToken(tokenDescriptor);

            return token;
        }

        public string CreateConfirmationEditPatientToken(User user)
        {
            string secretKey = configuration["Jwt:SecretKey"];
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
            var credentials = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);

            var tokenDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(new[]
                {
                    new Claim(JwtRegisteredClaimNames.Sub, user.Id.ToString()),
                    new Claim(JwtRegisteredClaimNames.Email, user.email.email),
                    new Claim("purpose", "confirmation_edit_patient")
                }),
                Expires = DateTime.UtcNow.AddMinutes(2), // Expira em 24 horas
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                Audience = configuration["Jwt:Audience"]
            };

            var handler = new JsonWebTokenHandler();
            string token = handler.CreateToken(tokenDescriptor);

            return token;
        }

        public string CreateConfirmationDeletePatientToken(User user)
        {
            string secretKey = configuration["Jwt:SecretKey"];
            var securityKey = new SymmetricSecurityKey(Encoding.UTF8.GetBytes(secretKey));
            var credentials = new SigningCredentials(securityKey, SecurityAlgorithms.HmacSha256);

            var tokenDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(new[]
                {
                    new Claim(JwtRegisteredClaimNames.Sub, user.Id.ToString()),
                    new Claim(JwtRegisteredClaimNames.Email, user.email.email),
                    new Claim("purpose", "confirmation_delete_patient")
                }),
                Expires = DateTime.UtcNow.AddMinutes(2), // Expira em 24 horas
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                Audience = configuration["Jwt:Audience"]
            };

            var handler = new JsonWebTokenHandler();
            string token = handler.CreateToken(tokenDescriptor);

            return token;
        }


    }

}
