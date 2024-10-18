using System;
using DDDSample1.Domain.Shared;


namespace DDDSample1.Domain.Patients
{

    public class MedicalRecordNumber : IValueObject
    {

        public string _medicalRecordNumber { get; private set; }

        public MedicalRecordNumber(string medicalRecordNumber)
        {
            validateMedicalRecordNumber(medicalRecordNumber);
            _medicalRecordNumber = medicalRecordNumber;
        }



        private void validateMedicalRecordNumber(string medicalRecordNumber)
        {
            if (string.IsNullOrWhiteSpace(medicalRecordNumber))
            {
                throw new ArgumentException("Invalid medical record number.");
            }

            if (medicalRecordNumber.Length != 12)
            {
                throw new ArgumentException("Medical record number must be 12 characters long.");
            }

            if (!int.TryParse(medicalRecordNumber.Substring(0, 4), out int year) || year > DateTime.Now.Year)
            {
                throw new ArgumentException("Invalid year in medical record number.");
            }

            if (!int.TryParse(medicalRecordNumber.Substring(4, 2), out int month) || month < 1 || month > 12)
            {
                throw new ArgumentException("Invalid month in medical record number.");
            }

            if (!int.TryParse(medicalRecordNumber.Substring(6, 6), out _))
            {
                throw new ArgumentException("Invalid sequential number in medical record number.");
            }
        }







    }
    








}