import { Component, EventEmitter, Output } from '@angular/core';
import { FormsModule} from '@angular/forms';
import { CommonModule } from '@angular/common';

@Component({
  selector: 'app-filter-operation-requests-component',
  standalone: true,
  imports: [FormsModule, CommonModule],
  templateUrl: './filter-operation-requests.component.html',
  styleUrl: './filter-operation-requests.component.css'
})
export class FilterOperationRequestsComponent {
  filter = {
    operationType: '',
    patientName: '',
    medicalRecordNumber: '',	
    startDate: '',
    endDate: '',
  };


  @Output() filterChanged = new EventEmitter<{ operationType: string; patientName: string; medicalRecordNumber: string; startDate: string; endDate: string }>();

  applyFilter(): void {
    this.filterChanged.emit(this.filter);
  }


  onSearch(event: Event): void {
    event.preventDefault();
    this.applyFilter();
  }

}
