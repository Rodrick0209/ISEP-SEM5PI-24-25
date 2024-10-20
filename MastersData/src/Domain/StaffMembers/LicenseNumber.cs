using System;
using System.Text.RegularExpressions;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.StaffMembers
{


    public class LicenseNumber : IValueObject
    {

        public string licenseNumber { get; private set; }

        public LicenseNumber(string licenseNumber)
        {
            validateLicenseNumber(licenseNumber);
            this.licenseNumber = licenseNumber;
        }

        private void validateLicenseNumber(string licenseNumber)
        {
            // Permite que licenseNumber seja vazio ou atenda aos formatos:
            // 1. 5 dígitos (xxxxx)
            // 2. E-5 dígitos (E-xxxxx)
            string pattern = @"^(E-\d{5}|\d{5})?$";

            if (!Regex.IsMatch(licenseNumber, pattern))
            {
                throw new ArgumentException("License number must be in the format 'xxxxx' or 'E-xxxxx', or it can be empty.");
            }
        }





    }







}