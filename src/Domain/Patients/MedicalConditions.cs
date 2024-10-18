using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class MedicalConditions : IValueObject
    {
        public string medicalConditions { get; private set; }

        public MedicalConditions(string medicalConditions)
        {
            validateMedicalConditions(medicalConditions);
            medicalConditions.Trim();
            this.medicalConditions = medicalConditions;
        }

        private void validateMedicalConditions(string medicalConditions)
        {
            if (string.IsNullOrWhiteSpace(medicalConditions))
            {
                throw new ArgumentNullException("Invalid medical conditions");
            }
        }
    }
}

