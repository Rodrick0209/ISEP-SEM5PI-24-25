using DDDSample1.Domain.OperationTypeLoggers;
using DDDSample1.Domain.OperationTypes;
using DDDSample1.Domain.Shared;
using Microsoft.EntityFrameworkCore;
using Microsoft.EntityFrameworkCore.Metadata.Builders;


namespace DDDSample1.Infrastructure.OperationTypesLoggers
{
    internal class OperationTypeLoggerEntityTypeConfiguration : IEntityTypeConfiguration<OperationTypeLogger>
    {

        public void Configure(EntityTypeBuilder<OperationTypeLogger> builder)
        {
            builder.HasKey(b => b.Id);
        }

    }
}