#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using Microsoft.Extensions.Logging.Configuration;

namespace DDDSample1.Domain.Patients
{
    public class Patient : Entity<PatientId>, IAggregateRoot
    {
        public FullName FullName { get; private set; }

        public DateOfBirth DateOfBirth { get; private set; }
        public Email Email { get; private set; }
        public PhoneNumber PhoneNumber { get; private set; }
        public Gender Gender { get; private set; }
        public EmergencyContact EmergencyContact { get; private set; }
        public MedicalRecordNumber MedicalRecordNumber { get; private set; }
        public MedicalConditions? MedicalConditions { get; private set; }
        public User.User? User { get; private set; }

        private Patient()
        {
            this.FullName = default!;
            this.DateOfBirth = default!;
            this.Gender = default!;
            this.Email = default!;
            this.PhoneNumber = default!;
            this.MedicalRecordNumber = default!;
            this.EmergencyContact = default!;
        }

        public Patient(FullName fullName, DateOfBirth dateOfBirth, Gender gender, Email email, PhoneNumber phoneNumber, EmergencyContact emergencyContact, MedicalRecordNumber medicalRecordNumber)
        {
            this.Id = new PatientId(Guid.NewGuid());
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Gender = gender;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContact = emergencyContact;
            this.MedicalRecordNumber = medicalRecordNumber;
        }
        
        public void ChangeFullName(FullName fullName)
        {
            this.FullName = fullName;
        }

        public void ChangeEmail(Email email)
        {
            this.Email = email;
        }

        public void ChangePhoneNumber(PhoneNumber phoneNumber)
        {
            this.PhoneNumber = phoneNumber;
        }

        public void ChangeMedicalConditions(MedicalConditions medicalConditions)
        {
            this.MedicalConditions = medicalConditions;
        }

        public void AssociateUser(User.User user)
        {
            this.User = user;
        }
        
    }
}


