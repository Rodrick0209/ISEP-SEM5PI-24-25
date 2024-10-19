#nullable enable
using System;
using DDDSample1.Domain.Patients;
using DDDSample1.Domain.Shared;

namespace DDDSample1.Domain.PatientLoggers
{
    public class PatientLogger : Entity<PatientLoggerId>
    {
        public PatientId PatientId { get; private set; }
        public string FullName { get; private set; }

        public string DateOfBirth { get; private set; }
        public string Email { get; private set; }
        public string PhoneNumber { get; private set; }
        public string Gender { get; private set; }
        public string EmergencyContact { get; private set; }
        public string MedicalRecordNumber { get; private set; }
        public string? MedicalConditions { get; private set; }
        public string TypeOfChange { get; private set; }
        public DateTime ModificationDate { get; private set; }

        private PatientLogger()
        {
            PatientId = default!;
            FullName = default!;
            DateOfBirth = default!;
            Gender = default!;
            Email = default!;
            PhoneNumber = default!;
            EmergencyContact = default!;
            MedicalRecordNumber = default!;
            TypeOfChange = default!;
        }

        public PatientLogger(PatientId patientId, string fullName, string dateOfBirth, string gender, string email, string phoneNumber,  string emergencyContact, string medicalRecordNumber, string? medicalConditions, string change, DateTime modificiationDate)
        {
            this.Id = new PatientLoggerId(Guid.NewGuid());
            this.PatientId = patientId;
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Gender = gender;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContact = emergencyContact;
            this.MedicalRecordNumber = medicalRecordNumber;
            this.MedicalConditions = medicalConditions;
            this.TypeOfChange = change;
            this.ModificationDate = modificiationDate;
        }

        public PatientLogger(string medicalRecordNumber, string typeOfChange, DateTime modificationDate)
        {
            this.Id = new PatientLoggerId(Guid.NewGuid());
            this.PatientId = default!;
            this.FullName = default!;
            this.DateOfBirth = default!;
            this.Gender = default!;
            this.Email = default!;
            this.PhoneNumber = default!;
            this.EmergencyContact = default!;
            this.MedicalRecordNumber = medicalRecordNumber;
            this.MedicalConditions = default;
            this.TypeOfChange = typeOfChange;
            this.ModificationDate = modificationDate;
        }
    }
}