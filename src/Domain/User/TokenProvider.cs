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

            var credentials = new SigningCredentials(securityKey,SecurityAlgorithms.HmacSha256);

            var tokerDescriptor = new SecurityTokenDescriptor
            {
                Subject = new ClaimsIdentity(
                [
                    new Claim(JwtRegisteredClaimNames.Sub,user.Id.ToString()),
                    new Claim(JwtRegisteredClaimNames.Email,user.email.email),
                    new Claim("role",user.role.role),
                ]),
                Expires = DateTime.UtcNow.AddMinutes(60),
                SigningCredentials = credentials,
                Issuer = configuration["Jwt:Issuer"],
                
                Audience = configuration["Jwt:Audience"]
            
            };
        
            var handler = new JsonWebTokenHandler();

            string token = handler.CreateToken(tokerDescriptor);

            return token;
    
    
        }
    

    }

}
