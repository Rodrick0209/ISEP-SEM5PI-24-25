#nullable enable
namespace DDDSample1.Domain.Patients
{
    public class SearchFiltersDto
    {
        public string? MedicalRecordNumber { get; set; }
        public string? Name { get; set; }
        public string? Email { get; set; }
        public string? DateOfBirth { get; set; }
    }
}