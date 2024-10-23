#nullable enable
using System;
using System.Collections.Generic;
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
        public string EmergencyContactName { get; private set; }
        public string EmergencyContactEmail { get; private set; }
        public string EmergencyContactPhoneNumber { get; private set; }
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
            EmergencyContactName = default!;
            EmergencyContactEmail = default!;
            EmergencyContactPhoneNumber = default!;
            MedicalRecordNumber = default!;
            TypeOfChange = default!;
        }

        public PatientLogger(PatientId patientId, string fullName, string dateOfBirth, string gender, string email, string phoneNumber,  string emergencyContactName, string emergencyContactEmail, string emergencyContactPhoneNumber, string medicalRecordNumber, string? medicalConditions, string change, DateTime modificiationDate)
        {
            this.Id = new PatientLoggerId(Guid.NewGuid());
            this.PatientId = patientId;
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Gender = gender;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContactName = emergencyContactName;
            this.EmergencyContactEmail = emergencyContactEmail;
            this.EmergencyContactPhoneNumber = emergencyContactPhoneNumber;
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
            this.EmergencyContactName = default!;
            this.EmergencyContactEmail = default!;
            this.EmergencyContactPhoneNumber = default!;
            this.MedicalRecordNumber = medicalRecordNumber;
            this.MedicalConditions = default;
            this.TypeOfChange = typeOfChange;
            this.ModificationDate = modificationDate;
        }
    }
}