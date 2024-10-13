using DDDSample1.Domain.Shared;
using System;

namespace DDDSample1.Domain.Patient
{
    public class EmergencyContact : IValueObject
    {
        public string emergencyContact { get; private set; }

        public EmergencyContact(string emergencyContact){
            validateEmergencyContact(emergencyContact);
            this.emergencyContact = emergencyContact;
        }

        private void validateEmergencyContact(string emergencyContact){
            if (string.IsNullOrEmpty(emergencyContact))
            {
                throw new ArgumentNullException("Invalid emergency contact");
            }
        }
    }
}