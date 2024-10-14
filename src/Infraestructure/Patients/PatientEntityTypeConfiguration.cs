using DDDSample1.Domain.Patient;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.Patients
{
    public class PatientEntityTypeConfiguration : IEntityTypeConfiguration<Patient>
    {
        public void Configure(EntityTypeBuilder<Patient> builder)
        {
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.DateOfBirth).Property(d => d.dateOfBirth);
            builder.OwnsOne(b => b.MedicalRecordNumber).Property(m => m._medicalRecordNumber);
            // builder.OwnsOne(b => b.MedicalConditions).Property(m => m._medicalConditions);
            builder.OwnsOne(b => b.EmergencyContact).Property(e => e.emergencyContact);
        }
    }
}
