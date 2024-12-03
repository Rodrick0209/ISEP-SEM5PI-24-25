using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.RoomTypes
{
    public class InternalCode : IValueObject
    {
        public string internalCode { get; private set; }

        public InternalCode(string internalCode)
        {
            validateInternalCode(internalCode);
            this.internalCode = internalCode;
        }

        private void validateInternalCode(string internalCode)
        {
            if (string.IsNullOrWhiteSpace(internalCode))
            {
                throw new ArgumentNullException("Invalid internal code");
            }

            if (internalCode.Length != 8 || !System.Text.RegularExpressions.Regex.IsMatch(internalCode, @"^[a-zA-Z0-9\-]+$"))
            {
                throw new BusinessRuleValidationException("Invalid internal code format");
            }
        }
    }
}