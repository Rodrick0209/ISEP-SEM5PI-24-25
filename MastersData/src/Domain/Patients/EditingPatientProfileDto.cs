#nullable enable

using System.Collections.Generic;

namespace DDDSample1.Domain.Patients
{
    public class EditingPatientProfileDto
    {
        public required string MedicalRecordNumber { get; set; }
        public string? FullName { get; set; }
        public string? Email { get; set; }
        public string? PhoneNumber  { get; set; }
        public string? MedicalConditions { get; set; }
        public List<string>? MedicalRecord { get; set; }
    }
}