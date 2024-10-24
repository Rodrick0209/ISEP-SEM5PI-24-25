using System;
using System.Collections.Generic;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Mvc.ApiExplorer;

namespace DDDSample1.Domain.Patients
{
    public interface IPatientService
    {
        Task<PatientDto> CreateAsync(CreatingPatientProfileDto dto);
        Task<PatientDto> UpdateAsync(EditingPatientProfileDto dto);
        Task DeleteAsync(string medicalRecordNumber);
        Task<PatientDto> GetByIdAsync(PatientId id);
        Task<List<PatientDto>> GetAllAsync();
        Task<List<ViewPatientDto>> SearchAsync(SearchFiltersDto dto);
        Task<PatientDto> GetByMedicalRecordNumberAsync(string medicalRecordNumber);
    }
}