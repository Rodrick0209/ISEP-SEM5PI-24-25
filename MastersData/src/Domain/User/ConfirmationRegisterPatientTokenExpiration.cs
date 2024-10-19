using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.User
{
    public class ConfirmationRegisterPatientTokenExpiration : IValueObject
    {
        public DateTime ExpirationDate { get; set; }

        public ConfirmationRegisterPatientTokenExpiration(DateTime expirationDate)
        {
            if(expirationDate < DateTime.Now) throw new ArgumentException("Expiration date cannot be in the past.");
            this.ExpirationDate = expirationDate;
        }
    }
}