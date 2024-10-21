using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User{


    public class Role : IValueObject{

        public string role  { get; private set; }


        public Role(string role){
            validateUserRole(role);
            this.role = role;
        }


        private void validateUserRole(string role){
            role = role.ToLower();
            List<string> roles = new List<string> {"admin", "doctor", "nurse","technician","patient"};
            if(string.IsNullOrEmpty(role) || !roles.Contains(role)){
                throw new ArgumentNullException("Invalid role");
            }
        }


    }


}