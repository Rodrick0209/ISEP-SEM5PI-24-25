using System;
using System.ComponentModel.DataAnnotations;
using DDDSample1.Domain.Shared;
using Microsoft.AspNetCore.Identity;

namespace DDDSample1.Domain.User
{
    public class User : Entity<UserId>, IAggregateRoot
    {
        public UserName username{get; private set;}
        public Email email{get; private set;}

        public Role role{get; private set;}


    }



}