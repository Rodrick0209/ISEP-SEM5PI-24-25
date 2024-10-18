using DDDSample1.Domain.PatientLoggers;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;

namespace DDDSample.Infrastructure.PatientLoggers
{

    internal class PatientLoggerEntityTypeConfiguration : IEntityTypeConfiguration<PatientLogger>
    {

        public void Configure(EntityTypeBuilder<PatientLogger> builder)
        {
            builder.HasKey(b => b.Id);
        }

    }
}