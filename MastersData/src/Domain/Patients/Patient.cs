#nullable enable
using System;
using System.Collections.Generic;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using EllipticCurve.Utils;
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

        public Patient(string fullName, string dateOfBirth, string gender, string email, string phoneNumber, string emergencyContact, string medicalRecordNumber)
        {
            this.Id = new PatientId(Guid.NewGuid());
            this.FullName = new FullName(fullName);
            this.DateOfBirth = new DateOfBirth(DateTime.Parse(dateOfBirth));
            this.Gender = new Gender(gender);
            this.Email = new Email(email);
            this.PhoneNumber = new PhoneNumber(phoneNumber);
            this.EmergencyContact = new EmergencyContact(emergencyContact);
            this.MedicalRecordNumber = new MedicalRecordNumber(medicalRecordNumber);
        }
        
        public void ChangeFullName(string fullName)
        {
            this.FullName = new FullName(fullName);
        }

        public void ChangeEmail(string email)
        {
            this.Email = new Email(email);
        }

        public void ChangePhoneNumber(string phoneNumber)
        {
            this.PhoneNumber = new PhoneNumber(phoneNumber);
        }

        public void ChangeMedicalConditions(string medicalConditions)
        {
            this.MedicalConditions = new MedicalConditions(medicalConditions);
        }

        public void AssociateUser(User.User user)
        {
            this.User = user;
        }
        
    }
}


