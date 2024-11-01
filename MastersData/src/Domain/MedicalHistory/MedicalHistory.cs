#nullable enable
using System;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.Patients
{
    public class MedicalHistory: Entity<MedicalHistoryId>
    {
        public MedicalConditions? MedicalConditions { get; set; }

        // Imlement the logic of appointment request to medical history

        public MedicalHistory()
        {
            this.Id = new MedicalHistoryId(Guid.NewGuid());
        }

        public void ChangeMedicalConditions(string medicalConditions)
        {
            this.MedicalConditions = new MedicalConditions(medicalConditions);
        }
    }
}