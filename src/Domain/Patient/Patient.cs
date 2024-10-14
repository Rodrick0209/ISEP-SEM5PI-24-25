#nullable enable
using System;
using DDDSample1.Domain.Shared;
using DDDSample1.Domain.Utils;
using Microsoft.AspNetCore.Identity;

namespace DDDSample1.Domain.Patient
{
    public class Patient : Entity<PatientId>, IAggregateRoot
    {
        public PatientId Id { get; private set; }
        public FullName FullName { get; private set; }
        
        public DateOfBirth DateOfBirth { get; private set; }
        public Email Email { get; private set; }
        public PhoneNumber PhoneNumber { get; private set; }
        public EmergencyContact? EmergencyContact { get; private set; }
        public MedicalRecordNumber MedicalRecordNumber { get; private set; }
        public User.User? User { get; private set; }


        public Patient(FullName fullName, DateOfBirth dateOfBirth, Email email, PhoneNumber phoneNumber, MedicalRecordNumber medicalRecordNumber, EmergencyContact? emergencyContact = null)
        {
            this.Id = new PatientId(Guid.NewGuid());
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContact = emergencyContact;
            this.MedicalRecordNumber = medicalRecordNumber;
        }

        public void AssociateUser(User.User? user){
            this.User = user;
        }
    }
}


