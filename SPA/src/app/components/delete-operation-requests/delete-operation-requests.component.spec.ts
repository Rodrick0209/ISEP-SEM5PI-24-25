import { ComponentFixture, TestBed } from '@angular/core/testing';

import { DeleteOperationRequestsComponent } from './delete-operation-requests.component';

describe('DeleteOperationRequestsComponent', () => {
  let component: DeleteOperationRequestsComponent;
  let fixture: ComponentFixture<DeleteOperationRequestsComponent>;

  beforeEach(async () => {
    await TestBed.configureTestingModule({
      imports: [DeleteOperationRequestsComponent]
    })
    .compileComponents();

    fixture = TestBed.createComponent(DeleteOperationRequestsComponent);
    component = fixture.componentInstance;
    fixture.detectChanges();
  });

  it('should create', () => {
    expect(component).toBeTruthy();
  });
});
