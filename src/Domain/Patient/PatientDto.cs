using System;

#nullable enable

namespace DDDSample1.Domain.Patient
{
    public class PatientDto
    {
        public Guid Id { get; set; }
        public string FullName { get; set; }
        public string DateOfBirth { get; set; }
        public string Email { get; set; }
        public string PhoneNumber { get; set; }
        public string? EmergencyContact { get; set; }
        public string MedicalRecordNumber { get; set; }

        public PatientDto(Guid id, string fullName, string dateOfBirth, string email, string phoneNumber, string medicalRecordNumber, string? emergencyContact = null)
        {
            this.Id = id;
            this.FullName = fullName;
            this.DateOfBirth = dateOfBirth;
            this.Email = email;
            this.PhoneNumber = phoneNumber;
            this.EmergencyContact = emergencyContact;
            this.MedicalRecordNumber = medicalRecordNumber;
        }
    }
}