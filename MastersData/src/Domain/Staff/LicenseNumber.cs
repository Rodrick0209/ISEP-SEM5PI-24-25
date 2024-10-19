using System;
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


        }




    }







}