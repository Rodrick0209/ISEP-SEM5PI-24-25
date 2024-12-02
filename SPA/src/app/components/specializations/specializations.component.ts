import { Component } from '@angular/core';
import { ListSpecializationComponent } from '../list-specialization/list-specialization.component';

@Component({
  selector: 'app-specializations',
  standalone: true,
  templateUrl: './specializations.component.html',
  imports : [ListSpecializationComponent],
  styleUrl: './specializations.component.css'
})
export class SpecializationsComponent {
  


}
