#nullable enable
using System;
using System.Runtime.CompilerServices;

namespace DDDSample1.Domain.Patient
{
    public class CreatingPatientProfileDto
    {
        public required string FirstName { get; set; }
        public required string LastName { get; set; }
        public required string FullName { get; set; }
        public required string DateOfBirth { get; set; }
        public required string Email { get; set; }
        public required string PhoneNumber { get; set; }
        public string? EmergencyContact { get; set; }
    }
}