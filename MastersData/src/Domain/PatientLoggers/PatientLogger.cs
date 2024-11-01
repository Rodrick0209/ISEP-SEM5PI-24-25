#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PatientLoggers
{
    public class PatientLogger : Entity<PatientLoggerId>, IAggregateRoot
    {
        public PatientId PatientId { get; private set; }
        public string MedicalRecordNumber { get; private set; }
        public string? MedicalConditions { get; private set; }

        public string TypeOfChange { get; private set; }
        public DateTime ModificationDate { get; private set; }

        private PatientLogger()
        {
            PatientId = default!;
            MedicalRecordNumber = default!;
            TypeOfChange = default!;
        }

        public PatientLogger(PatientId patientId, string medicalRecordNumber, string? medicalConditions, string typeOfChange)
        {
            this.Id = new PatientLoggerId(Guid.NewGuid());
            this.PatientId = patientId;
            this.MedicalRecordNumber = medicalRecordNumber;
            this.MedicalConditions = medicalConditions;
            this.TypeOfChange = typeOfChange;
            this.ModificationDate = DateTime.Now;
        }
    }
}