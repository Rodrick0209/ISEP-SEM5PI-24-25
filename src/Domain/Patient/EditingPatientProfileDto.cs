#nullable enable

namespace DDDSample1.Domain.Patient
{
    public class EditingPatientProfileDto
    {
        public required string MedicalRecordNumber { get; set; }
        public string? FullName { get; set; }
        public string? Email { get; set; }
        public string? PhoneNumber  { get; set; }
        // public string? MedicalRecord { get; set; }
        // public string? MedicalConditions { get; set; }
    }
}