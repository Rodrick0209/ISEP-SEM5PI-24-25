using System;

#nullable enable

namespace DDDSample1.Domain.Patient
{
    public class PatientDto
    {
        public Guid Id { get; set; }
        public string FullName { get; set; }
        public string DateOfBirth { get; set; }
        public string Gender { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
        public string? MedicalConditions { get; set; }
        public string EmergencyContact { get; set; }
        public string MedicalRecordNumber { get; set; }
            
        public PatientDto(Guid id, string fullName, string dateOfBirth, string gender, string email, string phoneNumber, string medicalRecordNumber, string emergencyContact, string? medicalConditions = null)
        {
            this.Id = id;
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Gender = gender;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContact = emergencyContact;
            this.MedicalRecordNumber = medicalRecordNumber;
            this.MedicalConditions = medicalConditions;
        }
    }
}