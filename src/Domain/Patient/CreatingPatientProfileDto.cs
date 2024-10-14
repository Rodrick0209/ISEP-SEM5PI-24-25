#nullable enable
using System.Runtime.CompilerServices;

namespace DDDSample1.Domain.Patient
{
    public class CreatingPatientProfileDto
    {
        public string FirstName { get; set; } = string.Empty;
        public string LastName { get; set; } = string.Empty;
        public string FullName { get; set; } = string.Empty;
        public string DateOfBirth { get; set; } = string.Empty;
        public string Email { get; set; } = string.Empty;
        public string PhoneNumber { get; set; } = string.Empty;
        public string? EmergencyContact { get; set; } = string.Empty;
    }
}