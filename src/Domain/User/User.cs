using System;
using System.ComponentModel.DataAnnotations;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Identity;

namespace DDDSample1.Domain.User
{
    public class User : Entity<UserId>, IAggregateRoot
    {
        public Email email{get; private set;}

        public Role role{get; private set;}

        public Password password { get; private set; }

        private User()
        {
        }

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


    }



}