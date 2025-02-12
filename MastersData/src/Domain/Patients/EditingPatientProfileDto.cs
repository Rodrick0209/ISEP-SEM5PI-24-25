#nullable enable

using System.Collections.Generic;

namespace DDDSample1.Domain.Patients
{
    public class EditingPatientProfileDto
    {
        public required string MedicalRecordNumber { get; set; }
        public string? Name { get; set; }
        public string? Email { get; set; }
        public string? PhoneNumber  { get; set; }
        public string? Street { get; set; }
        public string? PostalCode { get; set; }
        public string? City { get; set; }
        public string? Country { get; set; }
    }
}