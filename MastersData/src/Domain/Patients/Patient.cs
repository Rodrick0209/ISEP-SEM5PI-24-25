#nullable enable
using System;
using System.Collections.Generic;
using System.Net.Sockets;
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
        public Address Address { get; private set; }
        public EmergencyContact EmergencyContact { get; private set; }
        public MedicalRecordNumber MedicalRecordNumber { get; private set; }
        public MedicalHistory MedicalHistory { get; private set; }
        public User.User? User { get; private set; }

        private Patient()
        {
            this.FullName = default!;
            this.DateOfBirth = default!;
            this.Gender = default!;
            this.Email = default!;
            this.PhoneNumber = default!;
            this.Address = default!;
            this.MedicalRecordNumber = default!;
            this.EmergencyContact = default!;
        }

        public Patient(string fullName, string dateOfBirth, string gender, string email, string phoneNumber, string street, string postalCode, string city, string country, string emergencyContactName, string emergencyContactEmail, string emergencyContactPhoneNumber, string medicalRecordNumber)
        {
            this.Id = new PatientId(Guid.NewGuid());
            this.FullName = new FullName(fullName);
            this.DateOfBirth = new DateOfBirth(DateTime.Parse(dateOfBirth));
            this.Gender = new Gender(gender);
            this.Email = new Email(email);
            this.PhoneNumber = new PhoneNumber(phoneNumber);
            this.Address = new Address(street, postalCode, city, country);
            this.EmergencyContact = new EmergencyContact(emergencyContactName, 
                                                        emergencyContactEmail, 
                                                        emergencyContactPhoneNumber);
            this.MedicalHistory = new MedicalHistory();
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

        public void AssociateUser(User.User user)
        {
            this.User = user;
        }
        
    }
}


