#nullable enable
namespace DDDSample1.Domain.Patients
{
    public class MedicalHistoryDto
    {
        public string? MedicalConditions { get; set; }

        public MedicalHistoryDto(string medicalConditions)
        {
            this.MedicalConditions = medicalConditions;
        }
    }
}

