#nullable enable
using System;
using System.Runtime.CompilerServices;

namespace DDDSample1.Domain.Patients
{
    public class CreatingPatientProfileDto
    {
        public required string FirstName { get; set; }
        public required string LastName { get; set; }
        public required string FullName { get; set; }
        public required string DateOfBirth { get; set; }
        public required string Gender { get; set; }
        public required string Email { get; set; }
        public required string PhoneNumber { get; set; }
        public required string EmergencyContactName { get; set; }
        public required string EmergencyContactEmail { get; set; }
        public required string EmergencyContactPhoneNumber { get; set; }
    }
}