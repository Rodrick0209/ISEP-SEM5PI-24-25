using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using System;

namespace DDDSample1.Domain.Patients
{
    public class EmergencyContact : IValueObject
    {
        public FullName Name {get; private set;}
        public Email Email {get; private set;}
        public PhoneNumber PhoneNumber {get; private set;}
        
        private EmergencyContact(){}

        public EmergencyContact(string name, string email, string phoneNumber){
            this.Name = new FullName(name);
            this.Email = new Email(email);
            this.PhoneNumber = new PhoneNumber(phoneNumber);
        }
    }
}