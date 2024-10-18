using System;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.Staff
{


    public class LicenseNumber : IValueObject
    {

        public string _licenseNumber{ get; private set; }

        public LicenseNumber(string licenseNumber){
            validateLicenseNumber(licenseNumber);
            _licenseNumber = licenseNumber;


        }

        private void validateLicenseNumber(string licenseNumber){
            /*QUEM FIZER A Us de criar o staff tem de adicionar a regra
             de negocio do licence number */

        }




    }







}