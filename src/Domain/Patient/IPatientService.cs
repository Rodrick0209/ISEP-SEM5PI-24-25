using System.Threading.Tasks;

namespace DDDSample1.Domain.Patient
{
    public interface IPatientService
    {
        Task<PatientDto> CreateAsync(CreatingPatientProfileDto dto);
    }
}