using System;
using System.ComponentModel.DataAnnotations;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using Microsoft.AspNetCore.Identity;

namespace DDDSample1.Domain.User
{
    public class User : Entity<UserId>, IAggregateRoot
    {
        public Email email{get; private set;}

        public Role role{get; private set;}

        public Password password { get; private set; }

        public ResetPasswordToken ? resetPasswordToken { get; private set; }
        public ResetPasswordTokenExpiration ? resetPasswordTokenExpiration { get; private set; }
        

        private User()
        {
        }

        //falta o code aqui para o construtor
        public User (string email, string role)
        {
            this.Id = new UserId(Guid.NewGuid());
            this.email = new Email(email);
            this.role = new Role(role);


        }


        public void SetPassword(string password)
        {
            this.password = new Password(password);
        }

        public void SetResetPasswordToken(string resetPasswordToken, DateTime expirationDate)
        {
            this.resetPasswordToken = new ResetPasswordToken(resetPasswordToken);
            this.resetPasswordTokenExpiration = new ResetPasswordTokenExpiration(expirationDate);
        }

        public void ClearResetPasswordToken()
        {
            this.resetPasswordToken = null;
            this.resetPasswordTokenExpiration = null;
        }

        public int compareExpirationToken()
        {
            return DateTime.Compare(this.resetPasswordTokenExpiration.resetPasswordTokenExpiration, DateTime.UtcNow);
        }
    }


}