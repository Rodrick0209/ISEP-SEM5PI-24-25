using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Utils
{
    public class PhoneNumber : IValueObject
    {
        public string phoneNumber { get; private set; }

        public PhoneNumber(string phoneNumber)
        {
            validatePhoneNumber(phoneNumber);
            this.phoneNumber = phoneNumber;
        }

        private void validatePhoneNumber(string phoneNumber)
        {
            if (string.IsNullOrEmpty(phoneNumber))
            {
                throw new ArgumentNullException("Invalid phone number");
            }
        }
    }
}