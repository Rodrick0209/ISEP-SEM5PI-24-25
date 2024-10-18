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
        public DateTime ModificationDate { get; private set; }

        private PatientLogger(Patients.PatientId id)
        {
            PatientId = default!;
            FullName = default!;
            DateOfBirth = default!;
            Gender = default!;
            Email = default!;
            PhoneNumber = default!;
            EmergencyContact = default!;
            MedicalRecordNumber = default!;
        }

        public PatientLogger(PatientId patientId, string fullName, string dateOfBirth, string gender, string email, string phoneNumber,  string emergencyContact, string medicalRecordNumber, string? medicalConditions, DateTime modificiationDate)
        {
            this.PatientId = patientId;
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Gender = gender;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContact = emergencyContact;
            this.MedicalRecordNumber = medicalRecordNumber;
            this.MedicalConditions = medicalConditions;
            this.ModificationDate = modificiationDate;
        }
    }
}