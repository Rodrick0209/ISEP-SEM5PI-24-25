using DDDSample1.Domain.Patients;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample1.Infrastructure.Patients
{
    public class MedicalHistoryEntityTypeConfiguration : IEntityTypeConfiguration<MedicalHistory>
    {
        public void Configure(EntityTypeBuilder<MedicalHistory> builder)
        {
            builder.HasKey(b => b.Id);
            builder.OwnsOne(b => b.MedicalConditions).Property(m => m.medicalConditions).IsRequired(false);
        }
    }
}